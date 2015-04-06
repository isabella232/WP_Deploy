package com.whitepages.platformController.Commands

import java.io.PrintWriter
import java.net.URL
import java.nio.charset.Charset
import java.nio.file.{StandardOpenOption, Files}
import scala.sys.process._
import scala.language.postfixOps

trait ShellCommands {
  private val scpCommand = "scp -B -q -r " // TODO: factor out
  private val shipAgentUnitFile = false

  def doUpdate(home: String, container: String, server: String, version: String, nameMap: Map[String, String]) = {
    // create the destination directory
    s"ssh -i $home/.ssh/platform_id_rsa.private -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null core@$server sudo mkdir -p /wp/units" !;
    s"ssh -i $home/.ssh/platform_id_rsa.private -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null core@$server sudo chown -R core.core /wp/units" !;
    // get agent.service (version) from artifactory and write to tmp file
    val tempPath = Files.createTempFile("pc-", ".txt")
    val absolutePath = tempPath.toAbsolutePath
    //log.info(noId, s"temp file ${absolutePath.toString}")
    scala.util.control.Exception.catching(classOf[Throwable]).andFinally {
      Files.delete(tempPath)
    } {
      // copy agent.service to server via scp (using private key)
      new URL(s"http://jrepo0.util.pages:8081/artifactory/wp-search-release/com/whitepages/${nameMap(container)}_2.11/$version/${nameMap(container)}_2.11-$version.service") #> absolutePath.toFile !;
      s"$scpCommand -i $home/.ssh/platform_id_rsa.private -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null ${absolutePath.toString} core@$server:/wp/units/agent.service" !;
    }
    // write an install script to a temp file
    val tempPath1 = Files.createTempFile("pc-script-", ".txt")
    val writer = new PrintWriter(Files.newBufferedWriter(tempPath1, Charset.forName("UTF-8"), StandardOpenOption.APPEND))
    writer.println("sudo docker stop agent")
    writer.println("sudo docker rm agent")
    if (shipAgentUnitFile) writer.println("sudo cp /wp/units/agent.service /etc/systemd/system/")
    writer.println("sudo systemctl daemon-reload")
    writer.println("sudo systemctl enable agent")
    writer.println("sudo systemctl start agent")
    writer.close()
    // copy the install script to the server via scp (using a private key)
    scala.util.control.Exception.catching(classOf[Throwable]).andFinally {
      Files.delete(tempPath1)
    } {
      s"$scpCommand -i $home/.ssh/platform_id_rsa.private -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null ${tempPath1.toAbsolutePath.toString} core@$server:/wp/units/agent.install" !;
    }
    // run the install on the server via ssh (using a private key)
    s"ssh -i $home/.ssh/platform_id_rsa.private -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null core@$server sudo chmod a+x /wp/units/agent.install" !;
    s"ssh -i $home/.ssh/platform_id_rsa.private -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null core@$server sudo /wp/units/agent.install" !;
  }

}
